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
  !str
    ? ""
    : str
        .replace(/href="(.*?)"/gi, function(m, $1) {
          // if ($1.charAt(0) === "#") {
          //   return `href="/docs/${$1.substr(1)}"`;
          // } else {
          return `href="/erldoc/docs/${$1.replace(".html", "")}"`;
          // }
        })
        .replace(/onmouseover="(.*?)"/gi, "")
        .replace(/onmouseout="(.*?)"/gi, "")
        .replace(/\s\s+/g, " ")
        .replace(/\n/g, "");
// .replace('""', '"');

function pageParser(name, body) {
  const $ = cheerio.load(body);
  const summary = $(".module-summary-body").get().length
    ? $(".module-summary-body").text()
    : $(".file-summary-body").text();
  const description = $(".description-body").html();
  const funcs = $(".exports-body").html();

  return {
    name,
    summary,
    description: escaping(description),
    funcs: escaping(funcs)
  };
}

if (!fs.existsSync(dist)) {
  fs.mkdirSync(dist);
}

modulesList.forEach(moduleName => {
  request(
    {
      uri: source(moduleName)
    },
    function(error, response, body) {
      if (error) {
        console.log("%j", `File ${moduleName}.json ERRoR`, error);
        return;
      }

      const json = pageParser(moduleName, body);
      languages.forEach(lang => {
        const langFolder = path.resolve(dist, lang);
        if (!fs.existsSync(langFolder)) {
          fs.mkdirSync(langFolder);
        }
        fs.writeFile(
          path.resolve(langFolder, `${moduleName}.json`),
          JSON.stringify(json),
          function(err) {
            if (err) throw err;
            console.log(`File ${moduleName}.json created successfully.`);
          }
        );
      });
    }
  );
});
