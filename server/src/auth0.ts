import * as jwt from 'koa-jwt';

export module Auth0 {

  interface JwtOptions extends jwt.Options {
    audience?: string;
    issuer?: string;
    algorithms?: Array<string>;
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

  export const { jwtCheck } = {

    jwtCheck() {
      return jwt(options);
    }

  };

}
