(* This script tests the Dedukti plugin. *)

Require Dedukti.

Dedukti Set Destination "test".

Require
        Identity
        ImportA
        ImportB
        ImportC
        Cast
        Let
        Inductives
        Fixpoints
        Sections
        NestedModules
        NestedLibraries
        Reflexivity
        .

Require
        Coq.Init.Datatypes
        Coq.Init.Logic_Type
        Coq.Init.Logic
        Coq.Init.Notations
        Coq.Init.Peano
        Coq.Init.Prelude
        Coq.Init.Specif
        Coq.Init.Tactics
        Coq.Init.Wf
        
(*        Coq.Logic.Berardi*)
(*        Coq.Logic.ChoiceFacts*)
(*        Coq.Logic.ClassicalChoice*)
(*        Coq.Logic.ClassicalDescription*)
(*        Coq.Logic.ClassicalEpsilon*)
(*        Coq.Logic.ClassicalFacts*)
(*        Coq.Logic.Classical_Pred_Set*)
(*        Coq.Logic.Classical_Pred_Type*)
(*        Coq.Logic.Classical_Prop*)
(*        Coq.Logic.Classical_Type*)
(*        Coq.Logic.ClassicalUniqueChoice*)
(*        Coq.Logic.Classical*)
(*        Coq.Logic.ConstructiveEpsilon*)
(*        Coq.Logic.Decidable*)
(*        Coq.Logic.Description*)
(*        Coq.Logic.Diaconescu*)
(*        Coq.Logic.Epsilon*)
(*        Coq.Logic.Eqdep_dec*)
(*        Coq.Logic.EqdepFacts*)
(*        Coq.Logic.Eqdep*)
(*        Coq.Logic.FunctionalExtensionality*)
(*        Coq.Logic.Hurkens*)
(*        Coq.Logic.IndefiniteDescription*)
(*        Coq.Logic.JMeq*)
(*        Coq.Logic.ProofIrrelevanceFacts*)
(*        Coq.Logic.ProofIrrelevance*)
(*        Coq.Logic.RelationalChoice*)
(*        Coq.Logic.SetIsType*)
          .

Dedukti All Export.

