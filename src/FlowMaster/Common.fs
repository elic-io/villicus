module Flowmaster.Common

let requireStringVal (fieldName:string) value =
    match System.String.IsNullOrWhiteSpace value with
        | true -> raise (System.ArgumentNullException(fieldName))
        | false -> ()
