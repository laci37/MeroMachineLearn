/*
 * CMatrix.h
 *
 *  Created on: 2012.07.18.
 *      Author: mero
 */

#ifndef CMATRIX_H_
#define CMATRIX_H_
#include "CVector.h"

class CMatrix {
public:
	CMatrix();
	CMatrix(int rows, int cols);
	CMatrix(int rows, int cols, double initval);
	CMatrix(int rows, int cols, double initarr[]);
	CMatrix& operator=(const CMatrix that);
	CMatrix& operator*=(const CMatrix that);
	CMatrix& operator*=(const double that);
	CMatrix& operator+=(const CMatrix that);
	CMatrix& operator-=(const CMatrix that);
	const CMatrix operator*(const CMatrix that);
	const CMatrix operator*(const double that);
	const CMatrix operator+(const CMatrix that);
	const CMatrix operator-(const CMatrix that);
    const CVector operator[](const int subscript);
	const double get(int row, int col);
	void set(int row, int col, double value);
	virtual ~CMatrix();
private:
	double arr[];
	int rows, cols;
};

#endif /* CMATRIX_H_ */
