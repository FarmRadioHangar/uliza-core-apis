import { Entity, PrimaryColumn, Column } from 'typeorm';

@Entity()
export class Organizations {

    @PrimaryColumn('int', { generated: true })
    id: number;

    @Column()
    name: string;

}
