const express = require('express');
const cors = require('cors');
const path = require('path');
const { createProxyMiddleware } = require('http-proxy-middleware');

const app = express();

app.use(cors());

const failRatio = 0.0;
const errors = [401, 403, 500, 'timeout'];

app.use(
    [
      '/api/v3/profiles/:profileId/quotes',
      '/api/v1/transfers',
      '/api/v1/transfers/:transferId/cancel',
      '/api/v3/profiles/:profileId/transfers/:transferId/payments'
    ],
    (req, res, next) => {
  if (Math.random() < failRatio) {
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

// Serve static files from the parent directory (elm.js, index.html)
const staticPath = path.join(__dirname, '..');
app.use(express.static(staticPath));

// Fallback to index.html for client-side routing (SPA support)
app.get('*', (req, res) => {
  res.sendFile(path.join(staticPath, 'index.html'));
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Proxy server is running on port ${PORT}`);
  console.log(`Serving Elm application from ${staticPath}`);
});
