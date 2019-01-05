module Crm.Security.Cryptography
open System
open System.Text
open System.Security.Cryptography

module SHA512=
  let ofList (arr:string list)=
    use sha = SHA512.Create()
    let bytes =
      arr |> Array.ofList
          |> Array.collect Encoding.UTF8.GetBytes
          |> sha.ComputeHash
    BitConverter.ToInt64(bytes, 0)