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
      database: 'api_staging',
      user:     'username',
      password: 'password'
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
      database: 'api_production',
      user:     'username',
      password: 'password'
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
