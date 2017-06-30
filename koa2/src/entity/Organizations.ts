import { Table, PrimaryColumn, Column } from 'typeorm';

@Table()
export class Organizations {

    @PrimaryColumn('int', { generated: true })
    id: number;

    @Column()
    name: string;

}
