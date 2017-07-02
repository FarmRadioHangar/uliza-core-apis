module.exports = {
  test: {
    client: 'sqlite3',
    connection: {
      filename: ':memory:'
    },
    migrations: {
      tableName: 'migrations'
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
    useNullAsDefault: true
  },
  staging: {
    client: 'postgresql',
    connection: {
      database: 'staging',
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
      database: 'production',
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
