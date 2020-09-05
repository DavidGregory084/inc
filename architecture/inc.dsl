# To generate diagrams from this
# * Download the Structurizr CLI
# * Download plantuml
# * Navigate to this directory
# * Delete any *.png or *.puml files
# * Run the command `structurizr.sh -export -f plantuml -workspace inc.dsl`
# * Run the command `java -jar plantuml.jar`
#
workspace {
    model {
        user = person "User"
        incLanguage = softwareSystem "inc language" {
            incCompiler = container "inc compiler" {
                proto = component "/proto" "Declares protobuf definitions for the syntax trees."
                common = component "/common" "Declares syntax tree definitions, protobuf serialization and common functions."
                parser = component "/parser" "Parses a textual module into a syntax tree."
                resolver = component "/resolver" "Resolves fully qualified names for each identifier in the syntax trees."
                typechecker = component "/typechecker" "Infers types for every declaration and expression in the syntax tree."
                codegen = component "/codegen" "Generates JVM bytecode from syntax trees. Embeds a protobuf syntax tree as a class file attribute."
                main = component "/main" "Orchestrates compilation using the other modules. Fetches imported modules from the classpath."
            }
        }

        user -> main

        main -> parser

        main -> resolver

        main -> typechecker

        main -> codegen

        parser -> common

        resolver -> common

        typechecker -> common

        codegen -> common

        common -> proto
    }

    views {
        component incCompiler "Structure" "Dependency structure of the project" {
            include *
            animationStep user
            autoLayout
        }

        dynamic incCompiler "Compilation" "Diagram of the compilation process" {
            user -> main "Source files"
            main -> parser "Source text"
            parser -> resolver "Parsed syntax trees"
            resolver -> typechecker "Resolved syntax trees"
            typechecker -> codegen "Typechecked syntax trees"
            codegen -> main "Compiled classfiles"
        }
    }
}
