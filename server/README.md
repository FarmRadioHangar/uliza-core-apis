# farm-radio-apis

The purpose of the Farm Radio APIs is to provide a single point of access to 
all data resources within the organization's application infrastructure, and 
a unified interface for external applications to interact with the FRI service 
platform.

## Project setup

### Install

```
npm install
```

### Build

```
npm run build
```

### Database migrations

### Configuration

#### Environment variables

* `PORT`
* `DEBUG`
* `NODE_CONFIG_DIR`

##### Sample .env file

```
DEBUG=farm-radio-api*
```

#### Config file

##### Sample configuration file

```
# config/default.toml

[server]
name = "Farm Radio API Server"

[logs]
  [logs.access]
    path = "./tmp/logs/"
```

##### Format

The configuration file uses the [TOML language format](https://github.com/toml-lang/toml).

### Tests

```
npm test
```

### Run

```
npm start
```

## Logging

### Access logs

### Debug logging

```
DEBUG=farm-radio-api*
```

Here is an example of what the log output may look like.

```
  farm-radio-api:server Listening on port 3000 +0ms
  farm-radio-api:server 19:20:04.667Z  INFO access: handled: 200 (req_id=bec6e57a-5d7d-4b08-96b6-0a494785eeca, audit=true, remoteAddress=::ffff:127.0.0.1, remotePort=34456, latency=15, _audit=true, req.query="", req.version=*)
  farm-radio-api:server   GET /api/v1 HTTP/1.1
  farm-radio-api:server   host: localhost:3000
  farm-radio-api:server   user-agent: curl/7.47.0
  farm-radio-api:server   accept: */*
  farm-radio-api:server   --
  farm-radio-api:server   HTTP/1.1 200 OK
  farm-radio-api:server   content-type: application/json
  farm-radio-api:server   content-length: 30
  farm-radio-api:server   access-control-allow-origin: *
  farm-radio-api:server   access-control-allow-headers: Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, Api-Version, Response-Time
  farm-radio-api:server   access-control-allow-methods: GET
  farm-radio-api:server   access-control-expose-headers: Api-Version, Request-Id, Response-Time
  farm-radio-api:server   connection: Keep-Alive
  farm-radio-api:server   content-md5: 8WSaLe+pZBG3qEISB4dMVA==
  farm-radio-api:server   date: Sat, 24 Jun 2017 19:20:04 GMT
  farm-radio-api:server   server: Farm Radio API Server
  farm-radio-api:server   request-id: bec6e57a-5d7d-4b08-96b6-0a494785eeca
  farm-radio-api:server   response-time: 15
  farm-radio-api:server   --
  farm-radio-api:server   req.timers: {
  farm-radio-api:server     "_sanitizePath": 472,
  farm-radio-api:server     "restifyResponseHeaders": 143,
  farm-radio-api:server     "readBody": 507,
  farm-radio-api:server     "parseBody": 145,
  farm-radio-api:server     "get": 5731
  farm-radio-api:server   }
  farm-radio-api:server  +2s
```

## Documentation

```
npm run docs
```

### Online docs

* [GitHub hosted documentation](https://farmradiohangar.github.io/uliza-core-apis/server/docs/index.html)

## Contribute

### Technologies

* [Node.js](https://nodejs.org/)
* [restify](http://restify.com/)
* [TypeScript](http://www.typescriptlang.org/)

### Architecture, data formats, and protocols

* [REST](https://en.wikipedia.org/wiki/Representational_state_transfer)

### Coding style 

* See [TypeScript style guide and coding conventions](https://github.com/basarat/typescript-book/blob/master/docs/styleguide/styleguide.md#typescript-styleguide-and-coding-conventions)

### TypeDoc conventions

* [TypeDoc](http://typedoc.org/)

### API design standards and tools 

* http://www.vinaysahni.com/best-practices-for-a-pragmatic-restful-api
* https://github.com/WhiteHouse/api-standards
* http://dredd.readthedocs.io/en/latest/
