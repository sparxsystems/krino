using System;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IMorpheme : ILinguisticStructure, IEquatable<IMorpheme>
    {
        GrammarCharacter GrammarCharacter { get; }
    }
}
