# Matrix Events Directory

This repository is an unofficial directory of event types that can be found in the Matrix ecosystem. [View website](https://matrix.directory/).

If you're a client developer, this directory should help you in three ways:

1. If you see a custom event type that doesn't appear in [spec](spec.matrix.org/) and you wonder, _"huh, I wonder what that's for?"_ Then this repository should help answer that question.

2. You may want to make your client compatible with custom event types from other popular clients, but you may not want to search every open source project, looking through the code to figure out how their custom events work. You would rather have one place where you can find most custom event types.

3. Maybe you have an idea to implement chess! You could reinvent the wheel with your own custom event types, but this directory will help you be compatible with other developers with the same idea. That way, there might already be bots out there that your users could play chess against.

4. If you have written your own custom event types, you may want to encourage other developers to use the same for compatibility and interoperability. If you publish them here, it might be easier for them to find your custom event.

## How to browse the repository

You can view the rendered website [here](https://matrix-event.directory/).

## How to add custom events.

1. Fork this repository;

2. Write your custom event types in the `events/` folder. You can read [events/README.md](events/README.md) for more information.

3. Open `index.html` in the browser. This local website will verify if your JSON files are rendered properly.

4. Make a pull request to this repository.

Keep in mind that although this repository is meant to capture as many custom event types as possible, there may be a selection of which events get added. This is prone to opinions and subjective decisions. It may be possible that not all pull requests will receive the same level of requirements, but such requirements can include:

- Your custom event types should not be offensive, inflicting harm or encouraging to inflict harm, be intolerant or inappropriate towards an individual or multiple individuals, or be considered controversial in different manners.
- Your custom event types should be used in the Matrix ecosystem. It can be fun to design custom Matrix event types and figure out what they should look like, but it is more important to give client developers the confidence that they're writing parsers for custom event types that are actually being used. This doesn't mean they should be used **widely**, but usually at least one Matrix-compatible project should support the custom event type.
- Your custom event type's requirements should remain relatively static, and you should redefine the same event type over and over again. If you wish to update the specifications of your custom event type, make sure that the new definition is backwards compatible and would still support older versions. _(Usually, you can solve this by making new fields optional, and if the field is really needed, by adding a default if it is not present.)

## Disclaimer

This is an unofficial directory, managed by people in their free time.

Do not consider this a final list that dictates which custom event types are (in)valid, or that this is a comprehensive list that contains every custom event type being used in the Matrix ecosystem. The first one goes against the spirit of Matrix and the second one is practically unfeasible.

To moderate the custom event types, there are sections that recommend certain custom event types for certain features. _(Like, use THIS custom event type if you'd like to implement a chess bot!)_ Sometimes, it may happen that there is a multitude of custom event types that would enable that, and the moderation team may need to make a decision which event type they consider best suited for the task. Please do not pressure moderators into preferring your event types, or become hostile if they picked other event types than you would've liked.
