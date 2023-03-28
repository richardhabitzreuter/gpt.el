# gpt.el

gpt.el is an Emacs package that allows you to interact with OpenAI's GPT-3 API and get chat completions from it.

## Features

- Fetch chat completions from OpenAI's GPT-3 API using a custom model
- Parse JSON responses and insert them into the current buffer
- Bind a keyboard shortcut to fetch chat completions

## Installation

To use this package, you need to have an OpenAI account and an API key. You can find your API key in https://platform.openai.com/account/api-keys.

You also need the following dependencies:

- json.el
- url.el

You can install them using your preferred package manager, such as MELPA or ELPA.

To install gpt.el, you can either clone this repository and load it manually, or use a package manager like quelpa or straight.el.


## Setup

To setup gpt.el, you need to do the following steps:

1. Install the package using your preferred method, such as quelpa or straight.el. For example, using quelpa:

```elisp
(quelpa '(gpt :fetcher github :repo "richardhabitzreuter/gpt.el"))
```

2. Require the package in your config file:

```elisp
(require 'gpt)
```

3. Set your OpenAI API key as a variable:

```elisp
(setq gpt-apikey "your api key goes here")
```

4. Optionally, you can bind a keyboard shortcut to the `fetch-chatgpt-response` function. For example, to use `C-c C-p C-r`:

```elisp
(global-set-key (kbd "C-c C-p C-r") 'fetch-chatgpt-response)
```

5. Enjoy chatting with GPT-3 from any buffer!

## Usage

Type a message in any buffer and use the keyboard shortcut `C-c C-p C-r` to fetch a chat completion from GPT-3. The completion will be inserted after your message.

For example:

```
How are you today? C-c C-p C-r
How are you today? I'm doing well, thank you for asking.
```

You can also customize the model name and other parameters by modifying the `get-chat-completions-from-openai` function.

## License

This project is licensed under the MIT License. See the LICENSE file for details.


