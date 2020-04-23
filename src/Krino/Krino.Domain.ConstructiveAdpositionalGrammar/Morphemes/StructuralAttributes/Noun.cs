using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Noun attributes.
    /// </summary>
    public class Noun : EnumGroupBase
    {
        public Noun(EnumGroupBase parent) : base(parent)
        {
            Proper = new EnumValue(this);
            Common = new CommonNoun(this);
            Countable = new EnumValue(this);
            UnCountable = new EnumValue(this);
        }

        /// <summary>
        /// Name of a place or a person e.g. Wien, Georg, ...
        /// </summary>
        public EnumValue Proper { get; }

        /// <summary>
        /// Common name of a thing e.g. city.
        /// </summary>
        public CommonNoun Common { get; }

        /// <summary>
        /// Names of things which can be counted e.g. book.
        /// </summary>
        public EnumValue Countable { get; }

        /// <summary>
        /// Names of things which cannot be counted e.g. honesty, milk.
        /// </summary>
        public EnumValue UnCountable { get; }
    }
}
