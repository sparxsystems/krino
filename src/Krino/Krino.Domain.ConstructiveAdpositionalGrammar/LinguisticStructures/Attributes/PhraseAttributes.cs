using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class PhraseAttributes : EnumGroupBase
    {
        public PhraseAttributes(EnumGroupBase parent) : base(parent)
        {
            NounPhrase = new EnumValue(this);
            InfinitivePhrase = new EnumValue(this);
            GerundPhrase = new EnumValue(this);
            VerbPhrase = new EnumValue(this);
            PrepositionalPhrase = new EnumValue(this);
            AdverbialPhrase = new EnumValue(this);
        }

        public EnumValue NounPhrase { get; }
        public EnumValue InfinitivePhrase { get; }
        public EnumValue GerundPhrase { get; }
        public EnumValue VerbPhrase { get; }
        public EnumValue PrepositionalPhrase { get; }

        public EnumValue AdverbialPhrase { get; }
    }
}
