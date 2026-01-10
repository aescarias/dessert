package main

import (
	"github.com/alecthomas/kong"
)

const ExitFailure int = 1

var cli struct {
	Version kong.VersionFlag `help:"Show version and exit." short:"v"`

	Parse ParseCmd `cmd:"" help:"Parse an input file using a definition."`
	Ast   AstCmd   `cmd:"" help:"Show an abstract syntax tree (AST) of the definition."`
}

func main() {
	ctx := kong.Parse(&cli,
		kong.Name("dessert"),
		kong.Description("The Dessert runtime and CLI"),
		kong.ShortUsageOnError(),
		kong.ConfigureHelp(kong.HelpOptions{Compact: true}),
		kong.Vars{"version": "0.1.0"},
	)

	err := ctx.Run()
	ctx.FatalIfErrorf(err)
}
