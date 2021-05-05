### General
 * Complete migration to simplified class-based approach
   * refine relationship (and naming) between argument regexps and classes
 * variables may start with a 0, but allowing this conflicts with pattern matchers...
   * maybe a way out: variables can't be _only_ numbers!
 * improve images in README
 * Commands mentioned in docs but not available in current version
   * The #edit command has a number of options

### Testing
 * Need to add tests for at least all command types, if not all commands
 * Tests for shortened options should be more pervasive
 * Test for tintin-command-character and other configurable characters
 * Tests for individual functions in tintin-commands.el

### Commands that need work
 * The #history command has options
 * The #ignore command has a toggle
 * The #info command has a toggle plus "list" and "save"
 * The #log command has subcommands
 * The #map command has a ton going on
 * The #message command has a toggle
 * The #path command has options
 * There is a lot going on in #port
 * The #regexp command is in good shape but
   * verify regexp coverage for all in PCRE section of manual
 * The #scan command has options
 * The #screen command has options, some complex
 * The #snoop command has a toggle, and interacts with sessionname
 * The #unvariable command can be used for any number of variables
 * #[sessionname] is a thing....
   * there's support for sending commands to a session with #<sesssionname> {commands}:
   * and text can be evauated in a session with @<sessionname>{text}:

### Comments
 * Comment command stuff doesn't quite work per
   * http://ergoemacs.org/emacs/elisp_comment_command.html
 * Braces in #nop commands can create multiline constructs
   * This should largely be handled by current multiline approach
   * If this approach allows semicolons in the braces, then it could get weird
 * If there's no trailing semicolon should still highlight as comment just to end
