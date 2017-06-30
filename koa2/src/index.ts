import 'reflect-metadata';

import * as Koa from 'koa';
import * as Router from 'koa-router';
import * as bodyparser from 'koa-bodyparser';

import { createConnection } from 'typeorm';

createConnection().then(async connection => {

 let app: Koa = new Koa();
 const port: number = 3030;

 app.use(bodyparser());

 app.listen(port);

 console.log(`Koa application is up and running on port ${port}`);

}).catch(error => {
  console.log(console.log('Connection error'));
});
