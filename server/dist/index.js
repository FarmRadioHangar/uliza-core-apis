"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
require('dotenv').config();
const app_1 = require("./app");
app_1.default.listen(normalized(process.env.PORT || 3000));
function normalized(val) {
    const port = typeof val === 'string' ? parseInt(val, 10) : val;
    if (isNaN(port))
        return val;
    else if (port >= 0)
        return port;
    console.error('Bad port');
    process.exit(1);
}
