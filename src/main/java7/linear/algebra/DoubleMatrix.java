package linear.algebra;

import java.util.Arrays;
import java.util.List;
import com.google.common.primitives.Doubles;

/**
 * Created by oltyan on 2016.12.30.
 */
public class DoubleMatrix implements Matrix<Double> {
    private final int rowNum;
    private final int colNum;
    private final double[][] data;
    private final boolean isSquare;

    public DoubleMatrix(double[][] data) {
        this.rowNum = data.length;
        this.colNum = data[0].length;
        this.isSquare = rowNum == colNum;
        this.data = data;
    }

    @Override
    public int getColNum() {
        return colNum;
    }

    @Override
    public int getRowNum() {
        return rowNum;
    }

    @Override
    public List<Double> getRow(int row) {
        return Doubles.asList(data[row]);
    }

    @Override
    public List<Double> getCol(int col) {
        return this.transpose().getRow(col);
    }

    @Override
    public Double getElement(int row, int col) {
        return Double.valueOf(data[row][col]);
    }

    @Override
    public Matrix<Double> product(Matrix<Double> other) {
        if (colNum != other.getRowNum()) throw new RuntimeException("incompatible dimensions for matrix multiplication");
        double[][] result = new double[rowNum][other.getColNum()];
        Matrix<Double> sndComponent = other.transpose();
        for (int i = 0; i < rowNum; i++) {
            for (int j = 0; j < sndComponent.getRowNum(); j++) {
                result[i][j] = product(data[i], sndComponent.getRow(j));
            }
        }
        return new DoubleMatrix(result);
    }

    @Override
    public Matrix<Double> product(Double scalar) {
        double[][] result = Arrays.copyOf(data, data.length);
        for (int i = 0; i < rowNum; i++) {
            for (int j = 0; j < colNum; j++) {
                result[i][j] *= scalar;
            }
        }
        return new DoubleMatrix(result);
    }

    @Override
    public Matrix<Double> add(Matrix<Double> other) {
        double[][] result;
        if (dimEquals(other)) {
            result = new double[rowNum][colNum];
            for (int i = 0; i < rowNum; i++) {
                for (int j = 0 ; j < colNum; j++) {
                    result[i][j] = data[i][j] + other.getElement(i, j);
                }
            }
        } else {
            throw new RuntimeException("add : dimensions are imcompatible");
        }
        return new DoubleMatrix(result);
    }

    @Override
    public Matrix<Double> transpose() {
        double[][] result = new double[colNum][rowNum];
        for (int i = 0; i < colNum; i++) {
            for (int j= 0; j < rowNum; j++) {
                result[i][j] = data[j][i];
            }
        }
        return new DoubleMatrix(result);
    }

    @Override
    public Matrix<Double> extract(List<Integer> rows, List<Integer> cols) {
        double[][] result = new double[rows.size()][cols.size()];
        for (int i = 0; i < rows.size(); i++) {
            for (int j = 0; j < cols.size(); j++) {
                result[i][j] = data[rows.get(i)][cols.get(j)];
            }
        }
        return new DoubleMatrix(result);
    }

    @Override
    public Matrix<Double> inverse() {
        return null;
    }

    @Override
    public Double determinant() {
        double result = 0.0;
        if (!isSquare) {
            throw new RuntimeException("Only square matrix has determinant");
        } else if (rowNum < 1) {
            return result;
        } else if (rowNum == 1) {
            return data[0][0];
        } else if (rowNum == 2) {
            return data[0][0] * data[1][1] - data[1][0] * data[0][1];
        } else {
            int j2 = 0;
            double[][] helperM = new double[rowNum - 1][rowNum - 1];
            for (int j1 = 0; j1 < rowNum; j1++) {
                for (int i = 1; i < rowNum; i++) {
                    j2 = 0;
                    for (int j = 0; j < rowNum; j++) {
                        if (j != j1) {
                            helperM[i - 1][j2] = data[i][j];
                            j2 += 1;
                        }
                    }
                }
                result += (new DoubleMatrix(helperM)).determinant() * Math.pow(-1, j1 + 2) * data[0][j1];
            }
            return result;
        }
    }

    @Override
    public Matrix<Double> cofact() {
        return null;
    }

    @Override
    public Matrix<Double> adjugant() {
        return null;
    }

    private static double product(double[] row, List<Double> col) {
        double result = 0.0;
        if (row.length == col.size()) {
            for (int i = 0; i < row.length; i++) {
                result += row[i] * col.get(i);
            }
        } else {
            throw new RuntimeException("product of vectors: length are incompatible");
        }
        return result;
    }

    private boolean dimEquals(Matrix<Double> other) {
        return this.colNum == other.getColNum() && this.rowNum == other.getRowNum();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DoubleMatrix that = (DoubleMatrix) o;

        if (rowNum != that.rowNum) return false;
        if (colNum != that.colNum) return false;
        if (isSquare != that.isSquare) return false;
        return Arrays.deepEquals(data, that.data);
    }

    @Override
    public int hashCode() {
        int result = rowNum;
        result = 31 * result + colNum;
        result = 31 * result + Arrays.deepHashCode(data);
        result = 31 * result + (isSquare ? 1 : 0);
        return result;
    }
}
