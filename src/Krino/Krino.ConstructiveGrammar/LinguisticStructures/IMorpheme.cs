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

        IMorpheme BaseForm { get; internal set; }

        IMorpheme AddSuppletion(IMorpheme suppletion);

        IReadOnlyList<IMorpheme> Suppletions { get; }
    }
}
