using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class PhraseAttributes : EnumGroupBase
    {
        public PhraseAttributes(EnumGroupBase parent) : base(parent)
        {
            NounPhrase = new EnumValue(this);
            AdjectivePhrase = new EnumValue(this);
            InfinitivePhrase = new EnumValue(this);
            GerundPhrase = new EnumValue(this);
            VerbPhrase = new VerbPhraseAttributes(this);
            PrepositionalPhrase = new EnumValue(this);
            AdverbialPhrase = new EnumValue(this);
        }

        public EnumValue NounPhrase { get; }

        public EnumValue AdjectivePhrase { get; }

        /// <summary>
        /// Phrase consisting from the infinitive and possible verb modifiers.
        /// </summary>
        public EnumValue InfinitivePhrase { get; }
        public EnumValue GerundPhrase { get; }

        /// <summary>
        /// Auxiliary and verb words composing the verb item inside the verb element.
        /// </summary>
        public VerbPhraseAttributes VerbPhrase { get; }

        /// <summary>
        /// Preposition + noun.
        /// </summary>
        public EnumValue PrepositionalPhrase { get; }

        public EnumValue AdverbialPhrase { get; }
    }
}
