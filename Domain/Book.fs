namespace Excalibur.Domain

type Book =
    { Id: int
      Title: string
      Author: string option
      Path: string
      Tags: string option
      Comments: string option
      Checksum: string option
      AddedAt: string option
      Missing: bool }
