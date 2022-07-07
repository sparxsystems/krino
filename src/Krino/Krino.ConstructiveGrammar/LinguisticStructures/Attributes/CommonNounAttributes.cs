using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Common nouns attributes.
    /// </summary>
    public class CommonNounAttributes : EnumGroupBase
    {
        public CommonNounAttributes(EnumGroupBase parent) : base(parent)
        {
            Collective = new EnumValue(this);
            Concrete = new EnumValue(this);
            Abstract = new EnumValue(this);
        }

        /// <summary>
        /// A collective noun is the word used to represent a group of people, animals, or things.
        /// </summary>
        public EnumValue Collective { get; }

        /// <summary>
        /// Noun which refers to people or things which physically exist, can be seen, touched, smelled heard or tasted. E.g. dog, building, ...
        /// </summary>
        public EnumValue Concrete { get; }

        /// <summary>
        /// The name of the quality, action or state e.g. obedience, goodness.
        /// </summary>
        public EnumValue Abstract { get; }
    }
}
