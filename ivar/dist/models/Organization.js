"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
const typeorm_1 = require("typeorm");
let Organization = class Organization {
};
__decorate([
    typeorm_1.PrimaryGeneratedColumn(),
    __metadata("design:type", Number)
], Organization.prototype, "id", void 0);
__decorate([
    typeorm_1.Column(),
    __metadata("design:type", String)
], Organization.prototype, "name", void 0);
Organization = __decorate([
    typeorm_1.Entity()
], Organization);
exports.default = Organization;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiT3JnYW5pemF0aW9uLmpzIiwic291cmNlUm9vdCI6Ii9ob21lL2pvaGFubmVzL3dvcmsvdWxpemEtY29yZS1hcGlzL2l2YXIvc3JjLyIsInNvdXJjZXMiOlsibW9kZWxzL09yZ2FuaXphdGlvbi50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7Ozs7Ozs7OztBQUFBLHFDQUFpRTtBQUdqRSxJQUFxQixZQUFZLEdBQWpDO0NBUUMsQ0FBQTtBQUxHO0lBREMsZ0NBQXNCLEVBQUU7O3dDQUNOO0FBR25CO0lBREMsZ0JBQU0sRUFBRTs7MENBQ1k7QUFOSixZQUFZO0lBRGhDLGdCQUFNLEVBQUU7R0FDWSxZQUFZLENBUWhDO2tCQVJvQixZQUFZIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IHsgQ29sdW1uLCBFbnRpdHksIFByaW1hcnlHZW5lcmF0ZWRDb2x1bW4gfSBmcm9tICd0eXBlb3JtJztcblxuQEVudGl0eSgpXG5leHBvcnQgZGVmYXVsdCBjbGFzcyBPcmdhbml6YXRpb24ge1xuXG4gICAgQFByaW1hcnlHZW5lcmF0ZWRDb2x1bW4oKVxuICAgIHByaXZhdGUgaWQ6IG51bWJlcjtcblxuICAgIEBDb2x1bW4oKVxuICAgIHByaXZhdGUgbmFtZTogc3RyaW5nO1xuXG59XG4iXX0=