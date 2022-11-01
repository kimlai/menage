const base = require('./airtable');

// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
exports.handler = async (event) => {
  if (event.httpMethod !== "GET") {
    return {
      statusCode: 405
    };
  }

  try {
    const tasks = await base('tasks').select({
      sort: [{ field: "frequency", direction: "asc" }]
    }).all()
    const completions = await base('completions').select().all();

    return {
      statusCode: 200,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        tasks: tasks.map(c => c._rawJson),
        completions: completions.map(c => c._rawJson)
      })
    };
  } catch (error) {
    return { statusCode: 500, body: error.toString() }
  }
}
