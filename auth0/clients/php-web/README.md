# PHP web app login 

This sample demonstrates how to implement Auth0 authentication in a PHP application.

## Prerequisites

Before starting, make sure you have `composer` and `php` installed. Next, install the dependencies.

```bash
composer install
```
## Configuration

In the project's root directory, create and populate the `.env` file with the application's `AUTH0_CLIENT_ID`, `AUTH0_CLIENT_SECRET`, `AUTH0_DOMAIN`, and `AUTH0_CALLBACK_URL`. The client secret must be retrieved from the [Auth0 dashboard](https://manage.auth0.com/#/clients/vVSuj46PpkRF1m5FGxAmr1JuarNcwI0i/settings). 

```
AUTH0_CLIENT_ID=vVSuj46PpkRF1m5FGxAmr1JuarNcwI0i 
AUTH0_CLIENT_SECRET=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AUTH0_DOMAIN=farmradio.eu.auth0.com
AUTH0_CALLBACK_URL=http://localhost:3000/
```

## Running the app

```bash
php -S localhost:3000
```

The app will be served at [http://localhost:3000/](http://localhost:3000/).
