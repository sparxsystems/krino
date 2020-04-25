using Krino.Vertical.Utils.Collections;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Vertical.Utils.QConstructions
{
    public interface IQConstruction<T>
    {
        ITree<T> ActiveConstructions { get; }

        bool Add(T item);
    }
}
