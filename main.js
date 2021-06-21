import fs from "fs"
import path from "path"
import {Command} from "commander";
import {Elm} from "Main.elm"

const program = new Command()
program
    .option("-o, --out <path>", "Where to save the generated .elm files.", "src")
    .option("-n, --namespace <name>", "A namespace to prefix the generated modules with.", "Api")
    .option("-s, --spec <path>", "The path to the spec file.", "api.spec.json")

program.parse()

const options = program.opts()

let specPath = options.spec
let outputPath = options.out
let namespace = options.namespace

function crash(err) {
    console.error(err)
    process.exit(1)
}

fs.readFile(specPath, 'utf8', (err, data) => {
    if (err) return crash(err);

    let spec = JSON.parse(data);

    let main = Elm.Main.init()
    main.ports.output.subscribe(output => {
        if (output.error) return crash(output.error);
        for (let entry of output) {
            let filePath = path.resolve(outputPath, entry.path)
            fs.mkdir(path.dirname(filePath), {recursive: true}, (err) => {
                if (err) return crash(err);
            })
            fs.writeFile(filePath, entry.content, (err) => {
                if (err) return crash(err);
                console.log(filePath);
            })
        }
    })
    main.ports.input.send({
        namespace: namespace,
        spec: spec
    })
});

