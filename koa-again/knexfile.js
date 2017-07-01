module.exports = {
  development: {
    client: 'sqlite3',
    connection: {
      filename: './db.sqlite'
    }
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
