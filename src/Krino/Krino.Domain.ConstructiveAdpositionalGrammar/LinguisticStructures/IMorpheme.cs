using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IMorpheme : ILinguisticStructure, IEquatable<IMorpheme>
    {
        GrammarCharacter GrammarCharacter { get; }
    }
}
