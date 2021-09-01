#!/usr/bin/env node

import {Command} from "commander";
import {promises as fs} from "fs";
import yaml, {JSON_SCHEMA} from "js-yaml";
import {Elm} from "./src/Main.elm";
import path from "path";

const program = new Command()
program
    .option("-o, --out <path>", "Where to save the generated .elm files.", "src")
    .option("-n, --namespace <name>", "A namespace to prefix the generated modules with.", "Api")
    .option("-s, --spec <path>", "The path to the spec file.", "api.spec.yaml")

program.parse()

const options = program.opts()

let specPath = options.spec
let outputPath = options.out
let namespace = options.namespace

async function generate(specPath, outputPath, namespace) {
    let data = await fs.readFile(specPath, 'utf8')

    let spec;
    if (specPath.endsWith(".yml") || specPath.endsWith(".yaml")) {
        spec = yaml.load(data, {filename: specPath, schema: JSON_SCHEMA})
    } else {
        spec = JSON.parse(data);
    }

    let files = await new Promise((resolve, reject) => {
        let main = Elm.Main.init()
        main.ports.output.subscribe(output => {
            if (output.error) return reject(output.error);
            return resolve(output)
        })
        main.ports.input.send({
            namespace: namespace,
            spec: spec
        })
    });

    async function* write(files) {
        for (let file of await files) {
            let filePath = path.resolve(outputPath, file.path)
            await fs.mkdir(path.dirname(filePath), {recursive: true})
            await fs.writeFile(filePath, file.content);
            yield filePath
        }
    }

    for await (let file of write(files)) {
        console.log(file)
    }
}

generate(specPath, outputPath, namespace).catch(err => {
    console.error(err);
    process.exit(1);
});
