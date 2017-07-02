module.exports = {
  test: {
    client: 'sqlite3',
    connection: {
      filename: ':memory:'
    },
    migrations: {
      tableName: 'migrations'
    },
    seeds: {
      directory: './seeds/test'
    },
    useNullAsDefault: true
  },
  development: {
    client: 'sqlite3',
    connection: {
      filename: './db.sqlite'
    },
    migrations: {
      tableName: 'migrations'
    },
    seeds: {
      directory: './seeds/dev'
    },
    useNullAsDefault: true
  },
  staging: {
    client: 'postgresql',
    connection: {
      database: process.env.STAGING_DATABASE_NAME || 'api_staging',
      user: process.env.STAGING_DATABASE_USER,
      password: process.env.STAGING_DATABASE_PASSWORD
    },
    pool: {
      min: 2,
      max: 10
    },
    migrations: {
      tableName: 'migrations'
    }
  },
  production: {
    client: 'postgresql',
    connection: {
      database: process.env.PRODUCTION_DATABASE_NAME || 'api_production',
      user: process.env.PRODUCTION_DATABASE_USER,
      password: process.env.PRODUCTION_DATABASE_PASSWORD
    },
    pool: {
      min: 2,
      max: 10
    },
    migrations: {
      tableName: 'migrations'
    }
  }
};
