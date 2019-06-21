const fs = require("fs");
const path = require("path");
const request = require("request");
const cheerio = require("cheerio");

const defaultLanguage = "en";
const languages = ["en", "ru", "uk"];

const source = name => `http://erlang.org/doc/man/${name}.html`;

const modulesList = ["alarm_handler", "app", "application", "re", "array"];

const dist = path.resolve(__dirname, `../static/content`);
const escaping = str =>
  !str ? "" : str.replace(/\s\s+/g, " ").replace(/\n/g, "");

function pageParser(moduleName, body) {
  const $ = cheerio.load(body);
  const funcs = escaping($(".func-head").text());
  const titles = escaping($(".exports-body .title_link").text());
  return [moduleName, funcs, titles].filter(Boolean);
}

if (!fs.existsSync(dist)) {
  fs.mkdirSync(dist);
}
// languages.forEach(lang => {
//   const langFolder = path.resolve(dist, lang);
//   if (!fs.existsSync(langFolder)) {
//     fs.mkdirSync(langFolder);
//   }

// const lang = "en";
// const langFolder = path.resolve(dist, lang);
let result = modulesList.map(moduleName => {
  return new Promise((res, rej) => {
    request(
      {
        uri: source(moduleName)
      },
      function(error, response, body) {
        if (error) {
          rej("%j", `File ${moduleName}.json ERRoR`, error);
          return;
        }

        res(pageParser(moduleName, body));
      }
    );
  });
});

Promise.all(result).then(result => {
  fs.writeFile(
    path.resolve(dist, `tags.json`),
    JSON.stringify(result),
    function(err) {
      if (err) throw err;
      console.log(`File tags.json.json created successfully.`);
    }
  );
});
