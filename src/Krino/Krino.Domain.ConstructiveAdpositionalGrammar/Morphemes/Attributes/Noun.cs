using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Noun attributes.
    /// </summary>
    public class Noun : EnumGroupBase
    {
        public Noun(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 4)
        {
            Proper = new EnumValue(this, 1);
            Common = new CommonNoun(this, 2);
            Countable = new EnumValue(this, 3);
            UnCountable = new EnumValue(this, 4);
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
