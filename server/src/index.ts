require('dotenv').config();

import server from './app';

server.listen(normalized(process.env.PORT || 3000));

function normalized(val: number|string): number|string {
  const port: number = typeof val === 'string' ? parseInt(val, 10) : val;
  if (isNaN(port)) 
    return val;
  else if (port >= 0) 
    return port;
  console.error('Bad port');
  process.exit(1);
}
