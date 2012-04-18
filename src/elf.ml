(** elf.ml defines the stub functions provided by the external library Eric uses
    to manipulate Elf files; these functions are used by Elfrep *)
external    text_data: string                     -> int array = "text_data"
external text_address: string                     -> int       = "text_address"
external  text_offset: string                     -> int       = "text_offset"
external  update_text: string -> int -> int array -> unit      = "update_text"
