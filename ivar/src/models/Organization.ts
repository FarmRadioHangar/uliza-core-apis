import { Column, Entity, PrimaryGeneratedColumn } from 'typeorm';

@Entity()
export default class Organization {

    @PrimaryGeneratedColumn()
    private id: number;

    @Column()
    private name: string;

}
