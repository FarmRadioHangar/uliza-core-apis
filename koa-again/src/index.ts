import * as Koa        from 'koa';
import * as Router     from 'koa-router';
import * as bodyparser from 'koa-bodyparser';
import * as jwt        from 'koa-jwt';

const env  = process.env.NODE_ENV || 'development';

let app    = new Koa(),
    router = new Router();

router.get('/organizations', async ctx => {
  ctx.body = { collection: [] };
});

router.get('/protected', async ctx => {
  ctx.body = { message: 'This API is a teapot.' };
});

interface JwtOptions extends jwt.Options {
  audience?: string,
  issuer?: string,
  algorithms?: Array<string>
}

const options: JwtOptions = { 
  secret: require('jwks-rsa').koaJwtSecret({
    cache: true,
    rateLimit: true,
    jwksRequestsPerMinute: 5,
    jwksUri: 'https://farmradio.eu.auth0.com/.well-known/jwks.json'
  }),
  audience: 'https://dev.farmradio.fm/api/',
  issuer: 'https://farmradio.eu.auth0.com/',
  algorithms: ['RS256']
};

if ('test' !== env) {
  app.use(jwt(options));
}

app.use(bodyparser())
   .use(router.routes())
   .use(router.allowedMethods());

app.listen(8080);

console.log('Koa application is up and running');

export default app;
