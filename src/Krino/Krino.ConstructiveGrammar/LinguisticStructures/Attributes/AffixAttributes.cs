using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class AffixAttributes : EnumGroupBase
    {
        public AffixAttributes(EnumGroupBase parent) : base(parent)
        {
            Derivational = new EnumValue(this);
            Inflectional = new EnumValue(this);
        }

        /// <summary>
        /// Morphemes that changes the part of speech of the word. E.g. walk-able.
        /// </summary>
        /// <remarks>
        /// All prefices in english are derivational.
        /// https://parentingpatch.com/english-affixes-derivational-inflectional-prefixes-suffixes/
        /// </remarks>
        public EnumValue Derivational { get; }

        /// <summary>
        /// Morphemes that get added to a word, thus, adding a grammatical value to it. E.g. car-s.
        /// </summary>
        public EnumValue Inflectional { get; }
    }
}
