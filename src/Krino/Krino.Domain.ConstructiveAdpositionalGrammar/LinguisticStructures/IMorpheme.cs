using System;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public interface IMorpheme : ILinguisticStructure, IEquatable<IMorpheme>
    {
        GrammarCharacter GrammarCharacter { get; }
    }
}
