"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Utils = void 0;
class Utils {
    static extractInfo(input) {
        const parts = input.split(':');
        const params = parts.slice(0, 3)
            .concat([parts.slice(3)
                .join(':')]);
        params.splice(2, 1);
        return params;
    }
}
exports.Utils = Utils;
//# sourceMappingURL=utils.js.map