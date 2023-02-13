#!/usr/bin/env -S deno run --allow-read --allow-write --allow-env --allow-net
const http_service = "https://api.codelanguageconverter.com/convert_sample";
const data = {
  "from": "Python",
  "to": "Haskell",
  "code": (await Deno.readTextFile("code.py")).toString(),
};
const resp = await fetch(http_service, {
  "headers": {
    "accept": "application/json, text/plain, */*",
    "accept-language": "en-US,en;q=0.5",
    "authorization": "Bearer undefined",
    "cache-control": "no-cache",
    "content-type": "application/json",
    "pragma": "no-cache",
    "sec-ch-ua": '"Not_A Brand";v="99", "Brave";v="109", "Chromium";v="109"',
    "sec-ch-ua-mobile": "?0",
    "sec-ch-ua-platform": '"macOS"',
    "sec-fetch-dest": "empty",
    "sec-fetch-mode": "cors",
    "sec-fetch-site": "same-site",
    "sec-gpc": "1",
  },
  "referrer": "https://codelanguageconverter.com/",
  "referrerPolicy": "strict-origin-when-cross-origin",
  "body": JSON.stringify(data),
  "method": "POST",
  "mode": "cors",
  "credentials": "include",
});

const result = await resp.json();
Deno.writeTextFile("code.hs", result.data.result);
