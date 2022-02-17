using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class FreeMorphemeAttributes : EnumGroupBase
    {
        public FreeMorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexical = new LexicalMorphemeAttributes(this);
            Functional = new FunctionalMorphemeAttribute(this);
        }

        public LexicalMorphemeAttributes Lexical { get; }

        public FunctionalMorphemeAttribute Functional { get; }
    }
}
