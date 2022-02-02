using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
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
        /// Semantic attributes.
        /// </summary>
        public NounSememes Sememe { get; }
    }
}
