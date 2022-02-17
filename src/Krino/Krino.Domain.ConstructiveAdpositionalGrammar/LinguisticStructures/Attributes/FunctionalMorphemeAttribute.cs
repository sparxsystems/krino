using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class FunctionalMorphemeAttribute : EnumGroupBase
    {
        public FunctionalMorphemeAttribute(EnumGroupBase parent) : base(parent)
        {
            Pronoun = new PronounAttributes(this);
            Determiner = new DeterminerAttributes(this);
            Preposition = new EnumValue(this);
            Postposition = new EnumValue(this);
            Conjunction = new ConjunctionAttributes(this);
            Interjection = new EnumValue(this);
        }

        public PronounAttributes Pronoun { get; }
        public DeterminerAttributes Determiner { get; }
        public EnumValue Preposition { get; }
        public EnumValue Postposition { get; }
        public ConjunctionAttributes Conjunction { get; }
        public EnumValue Interjection { get; }
    }
}
