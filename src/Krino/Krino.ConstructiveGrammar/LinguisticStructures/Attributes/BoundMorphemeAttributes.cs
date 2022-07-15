using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class BoundMorphemeAttributes : EnumGroupBase
    {
        public BoundMorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new AffixAttributes(this);
            Suffix = new AffixAttributes(this);
            Root = new EnumValue(this);
        }

        public AffixAttributes Prefix { get; }
        public AffixAttributes Suffix { get; }

        /// <summary>
        /// Morphemes creating a base of a word which cannot stand alone. Prefixes or/and suffixes are needed to create a word.
        /// </summary>
        public EnumValue Root { get; }
    }
}
