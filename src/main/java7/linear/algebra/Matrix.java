package linear.algebra;

import java.util.List;

/**
 * Created by oltyan on 2016.12.30.
 */
public interface Matrix<T extends Number> {
    int getColNum();
    int getRowNum();
    List<T> getRow(int row);
    List<T> getCol(int col);
    T getElement(int row, int col);
    Matrix<T> product(Matrix<T> other);
    Matrix<T> product(T scalar);
    Matrix<T> add(Matrix<T> other);
    Matrix<T> transpose();
    Matrix<T> extract(List<Integer> rows, List<Integer> cols);
    Matrix<T> inverse();
    T determinant();
    Matrix<T> cofact();
    Matrix<T> adjugant();
}
