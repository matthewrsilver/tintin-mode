### General
 * Seems somewhat slow to load

 * Complete migration to simplified class-based approach
   * refine relationship between argument regexps and classes
   * consider a :final slot
   * eliminate double quoting

### Testing
 * Need to add tests for at least all command types, if not all commands
 * Test for tintin-command-character
 * Particular commands requiring expanded testing:
   * #script

### Commands that need work
 * The #buffer command's info and find modes need a look
 * The #chat command has many modes
 * The #config command has many modes, particularly toggles
 * It looks like #cursor does some weird stuff
 * The #daemon command has modes
 * The #debug command has a toggle with "log" as well
 * The #edit command has a number of options
 * The #format command is in good shape but:
   * additional matchers for the time (%t) format specifier??
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

### Misc
 * braced variable usage flows onto next line!?
 * closing brace after table gets highlighted?? (e.g. "{${x[]}}")
 * enable configuration of ~ symbol, as tintin does
 * use slightly different color for contents of square braces??
 * tintin files must start with a valid command; consider highlighting failures here?
 * add autoload for .tin files, not just .tt
