namespace Villicus.Domain

    module Result =
      let ofOption onMissing = function
        | Some x -> Ok x
        | None   -> Error onMissing
