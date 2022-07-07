using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class LexicalMorphemeAttributes : EnumGroupBase
    {
        public LexicalMorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Noun = new NounAttributes(this);
            Verb = new VerbAttributes(this);
            Adjective = new AdjectiveAttributes(this);
            Adverb = new AdverbAttributes(this);
            Numeral = new NumeralAttributes(this);
        }


        public NounAttributes Noun { get; }

        public VerbAttributes Verb { get; }

        public AdjectiveAttributes Adjective { get; }

        public AdverbAttributes Adverb { get; }

        public NumeralAttributes Numeral { get; }
    }
}
