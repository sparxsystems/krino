using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Attributing.Structural
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
        /// Name of to denote a number of persons or things e.g. army, crowed, family.
        /// </summary>
        public EnumValue Collective { get; }

        /// <summary>
        /// Noun which refers to people or things which physically exist, can be seen, touched, smelled heard or tasted. E.g. dog, building, ...
        /// </summary>
        public EnumValue Concrete { get; }

        /// <summary>
        /// The name of the quaity, action or state e.g. obedience, goodness.
        /// </summary>
        public EnumValue Abstract { get; }
    }
}
