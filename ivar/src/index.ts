import 'reflect-metadata';

import { Container } from 'typescript-ioc';

import App from './App';

const app: App = Container.get(App);

app.start();
