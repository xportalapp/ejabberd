"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const sdk_native_auth_server_1 = require("@multiversx/sdk-native-auth-server");
const utils_1 = require("./utils");
const fs = require('fs');
let logs = '';
async function bootstrap() {

    const nativeAuthServer = new sdk_native_auth_server_1.NativeAuthServer({
        apiUrl: 'https://devnet-api.multiversx.com',
        maxExpirySeconds: 1200,
    });
    process.stdin.on('data', async (data) => {

        const dataString = data.toString();
        logs += data;
        const [method, address, token] = utils_1.Utils.extractInfo(dataString);
        if (!method.endsWith('auth')) {
            logs += `\n method is not auth: ${method}`
            writeResult(false);
            return;
        }
        try {
            const result = await nativeAuthServer.validate(token);
            if (result.address !== address) {
                logs += `\n address do not match`
                writeResult(false);
                return;
            }
        }
        catch (_a) {
            logs += `\n error: ${_a}`
            writeResult(false);
            return;
        }
        logs += `success`;
        writeResult(true);
    });
}
const writeResult = (isSuccess) => {
    let buffer = Buffer.from([0, 2, 0, 1]);
    if (!isSuccess) {
        buffer = Buffer.from([0, 2, 0, 0]);
    }
    process.stdout.write(buffer);
    fs.writeFile('/opt/ejabberd/logs/log.txt', logs, err => {
        if (err) {
            console.error(err);
        }
        // file written successfully
    });
};
bootstrap();
//# sourceMappingURL=main.js.map