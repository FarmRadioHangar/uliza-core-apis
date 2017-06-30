import { Connection, EntityManager, MigrationInterface, QueryRunner, TableSchema } from 'typeorm';

export class CreateOrganizations1498857755868 implements MigrationInterface {

    public async up(queryRunner: QueryRunner): Promise<any> {
      queryRunner.createTable(new TableSchema('organizations', [
        {
          name: 'id',
          type: 'integer',
          isGenerated: true,
          isPrimary: true
        },
        {
          name: 'name',
          type: 'character varying(255)',
          isNullable: false
        }
      ]));
    }

    public async down(queryRunner: QueryRunner): Promise<any> {
      queryRunner.dropTable('organizations');
    }

}
