const fs = require("fs");
const path = require("path");
const request = require("request");
const cheerio = require("cheerio");

const defaultLanguage = "en";
const languages = ["en", "ru", "uk"];

const source = name => `http://erlang.org/doc/man/${name}.html`;

const dist = path.resolve(__dirname, `../static/content`);

fs.readdirSync(path.resolve(__dirname, `../static/content/en`)).forEach(
  file => {
    console.log(file);

    fs.readFile(
      path.resolve(__dirname, "../static/content/en", file),
      (err, text) => {
        const json = JSON.parse(text);
        json.name = file
          .split(".")
          .slice(0, -1)
          .join(".");

        console.log("JSON.stringify(json)", JSON.stringify(json));
        // fs.writeFile(
        //   path.resolve(__dirname, "../static/content/en", file),
        //   JSON.stringify(json),
        //   function(err) {
        //     if (err) throw err;
        //     console.log(`File ${file}.json created successfully.`);
        //   }
        // );
      }
    );
  }
);

// modulesList.forEach((moduleName, index) => {
//   setTimeout(() => {
//     request(
//       {
//         uri: source(moduleName)
//       },
//       function(error, response, body) {
//         if (error) {
//           console.log("%j", `File ${moduleName}.json ERRoR`, error);
//           return;
//         }

//         const json = pageParser(body);
//         languages.forEach(lang => {
//           const langFolder = path.resolve(dist, lang);
//           if (!fs.existsSync(langFolder)) {
//             fs.mkdirSync(langFolder);
//           }
//           fs.writeFile(
//             path.resolve(langFolder, `${moduleName}.json`),
//             JSON.stringify(json),
//             function(err) {
//               if (err) throw err;
//               console.log(`File ${moduleName}.json created successfully.`);
//             }
//           );
//         });
//       }
//     );
//   }, index * 1000);
// });
