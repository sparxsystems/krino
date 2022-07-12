using Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Noun attributes.
    /// </summary>
    public class NounAttributes : EnumGroupBase
    {
        public NounAttributes(EnumGroupBase parent) : base(parent)
        {
            Proper = new EnumValue(this);
            Common = new CommonNounAttributes(this);
            Countable = new EnumValue(this);
            UnCountable = new EnumValue(this);
            Possessive = new EnumValue(this);
            Sememe = new NounSememes(this);
        }

        /// <summary>
        /// Name of a place or a person e.g. Wien, Georg, ...
        /// </summary>
        public EnumValue Proper { get; }

        /// <summary>
        /// Common name of a thing e.g. city.
        /// </summary>
        public CommonNounAttributes Common { get; }

        /// <summary>
        /// Names of things which can be counted e.g. book.
        /// </summary>
        public EnumValue Countable { get; }

        /// <summary>
        /// Names of things which cannot be counted e.g. honesty, milk.
        /// </summary>
        public EnumValue UnCountable { get; }

        /// <summary>
        /// E.g. Brandon’s book.
        /// </summary>
        public EnumValue Possessive { get; }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public NounSememes Sememe { get; }
    }
}
