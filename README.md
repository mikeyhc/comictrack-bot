# comictrack-bot
Discord bot for tracking comic ownership

# inital setup
In a clean shell (running with a -sname or -name) run
```
% Node - a list of nodes to install to, likely just [node()]
% DiscordAppId - the app id for the discord bot you are using
% DiscordBotToken - the bot token for the same app
comictrack_bot_app:install(Nodes, DiscordAppId, DiscordBotToken).
```

This will create all the mnesia databases as well as installing the discord
commands. This process is pretty ad-hoc so check the command output for anything
fishy.
