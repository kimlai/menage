const base = require('./airtable');

// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  if (event.httpMethod !== "DELETE") {
    return {
      statusCode: 405
    };
  }

  try {
    const res = await base('completions').destroy(event.queryStringParameters.id);
    return {
      statusCode: 200,
      body: JSON.stringify("ok")
    };
  } catch (error) {
    return { statusCode: 500, body: error.toString() }
  }
}
