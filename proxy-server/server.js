const express = require('express');
const cors = require('cors');
const { createProxyMiddleware } = require('http-proxy-middleware');

const app = express();

app.use(cors());

const failRatio = 0.2; // 20% failure rate
const errors = [401, 403, 500, 'timeout'];

app.use('/api/v3/profiles/:profileId/quotes', (req, res, next) => {
  if (req.method === 'POST' && Math.random() < failRatio) {
    const error = errors[Math.floor(Math.random() * errors.length)];
    if (error === 'timeout') {
      // Simulate a timeout by not calling next() or sending a response
      return;
    } else {
      res.status(error).send(`Simulated ${error} error`);
      return;
    }
  }
  next();
});

app.use('/api', createProxyMiddleware({
  target: 'https://api.transferwise.com',
  changeOrigin: true,
  pathRewrite: {
    '^/api': '', // Remove /api prefix when forwarding to target
  },
  onProxyReq(proxyReq, req, res) {
    // Modify the request headers if needed
  },
  onProxyRes(proxyRes, req, res) {
    // Modify the response headers if needed
  }
}));

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Proxy server is running on port ${PORT}`);
});
