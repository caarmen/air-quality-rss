/**
 * This script starts a mock server.
 * It takes two command line arguments:
 * 1. The path to the file to be served.
 * 2. The HTTP status code to be returned.
 * The server listens on port 8000 and serves the file at the root URL.
 */

import http from 'http';
import path from 'path';
import fs from 'fs';

// parse the command line arguments
const args = process.argv.slice(2);
const responseFile = args[0];
const responseHttpStatus = args[1];
const delay_s = args.length === 2 ? null : args[2]

// Verify the command line arguments
if (!responseFile || !responseHttpStatus) {
    console.error(`Usage: node ${path.basename(__filename)} <response file> <http status code> [delay in seconds]`);
    process.exit(1);
}

// Configure the server to serve the file with the specified HTTP status code
// at the root URL.
const server = http.createServer((req, res) => {
    console.log(`Received request for ${req.url}`);
    res.writeHead(responseHttpStatus, { 'Content-Type': 'application/json' });
    const body = fs.readFileSync(responseFile, 'utf-8');
    setTimeout(() =>{
        res.end(body);
    }, delay_s * 1000);
});

// Start the server.
server.listen(8000, "0.0.0.0", () => {
    console.log(`Mock server listening on port 8000, serving ${responseFile} with status code ${responseHttpStatus}`);
});
