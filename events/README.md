# Adding new events

The file `_dir.json` contains a comprehensive list of all events that can be found in this folder.
When adding new event types, make sure to add your event type names to the `all` key in that file. Please keep the list sorted alphabetically.

**Please do not** change the `sets` key in the `_dir.json` file. This set is curated with care by the moderation team and is not open to change in pull requests that add new custom event types.

Once you have done that, you can add a new file named like your event type with `.json` at the end. For example, if I want to add the new event `org.example.game.score`, I would create a file called `org.example.game.score.json`.

In this file, you create an object the following keys:

- `name` is the name of your event type. (For example: `org.example.game.score`)
- `description` is a description of what your event type is used for.
- `sources` is an optional list of URLs where you can refer to documentation, relevant discussions and information that can provide people with more info about the event type.
- `eventType` is the type of event this can be sent as. This is one of `message`, `state` or `ephemeral`.
- `state` is an optional key that you only have to include if `eventType` is `state`. In that case, this key is a string explaining what the value of the state key is intended to be.
- `content` is a list of fields that your event's `content` key will have.
- `objects` is a dictionary of objects that may be used in your event. Each object has its name as a key and a list of fields as its value.
- `examples` is a list of examples that may be displayed on the page. Every example is an object containing a `name` key, a `description` key as a list of strings and an `example` key which is the actual object that you could send to the API according to your definition.

Make sure to open `index.html` from time to time. The page will automatically load and decode your custom event, and it should give you helpful tips and explain what you're missing if your file isn't complete.

## List of fields

Both the `content` and the objects have a list of fields, which is a description of what the objects look like.

Every field has the following keys:

- `name` is the name of the field and the key that they `content` object will have.
- `type` is the expected value belonging to the key. The option here are `int`, `string`, `enum` and `object`.
    - `int` is a whole number ranging from -(2\*\*53) + 1 to (2\*\*53) - 1.
    - `string` is a string.
    - `enum` is a string that has a limited set of allowed values.
    - `object` is a custom object that you may define in the `objects` key of your custom event type file.
- `key` relates to a value that adds more information to the `type` key. It is not always required.
    - If `type` is `int`, this key will be ignored.
    - If `type` is `string`, this key will be ignored.
    - If `type` is `enum`, this key is a list of strings. The strings are the only allowed values.
    - If `type` is `object`, this key is the name of the object. It must correspond to that key in your `objects` object in your custom event type file.
- `required` is a boolean that tells whether users are required to give this field.
- `description` is a value that explains what the key is meant for. It will also note any optional default values.

In the `type` key, you may also wrap the values around `[]` and `{}` symbols respectively, indicating lists and key-value dictionaries respectively. For example, `[string]` indicates a list of strings, `{int}` indicates objects like `{'foo': 3, 'bar': 2}`, `[[int]]` indicates lists of lists of integers like `[[1, 2], [3], [4, 5, 6]]` and `[[{{[{object}]}}]]` indicates a.... well, a deeply nested object.

**Tip:** the format is relatively straightforward, so you can look at other events in this folder as an example!

