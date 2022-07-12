using System;
using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IMorpheme : ILinguisticStructure, IEquatable<IMorpheme>
    {
        GrammarCharacter GrammarCharacter { get; }

        /// <summary>
        /// Binding rules for a bound morpheme.
        /// </summary>
        IMorphemeBinding Binding { get; }

        List<Suppletion> Suppletions { get; }
    }
}
